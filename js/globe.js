function clamp(value, min, max) {
    return value < min ? min : value > max ? max : value;
}

class GlobeCanvas {
    compileShader(
        /** @type GLenum */
        kind,
        /** @type string */
        source
    ) {
        const gl = this.gl;
        const shader = gl.createShader(kind);
        gl.shaderSource(shader, source);
        gl.compileShader(shader);
        if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
            console.error(gl.getShaderInfoLog(shader));
            return null;
        }
        return shader;
    }

    linkProgram(
        /** @type WebGLShader */
        vert,
        /** @type WebGLShader */
        frag
    ) {
        const gl = this.gl;
        const program = gl.createProgram();
        gl.attachShader(program, vert);
        gl.attachShader(program, frag);
        gl.linkProgram(program);
        if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
            console.error(gl.getProgramInfoLog(program));
            return null;
        }
        return program;
    }

    constructor(
        /** @type HTMLCanvasElement */
        canvasElement
    ) {
        /** @type HTMLCanvasElement */
        this.canvasElement = canvasElement;
        /** @type WebGLRenderingContext */
        this.gl = this.canvasElement.getContext("webgl");
        const gl = this.gl;

        const vert = this.compileShader(
            gl.VERTEX_SHADER,
            `
attribute vec2 pos;
varying vec2 pos_;
uniform vec2 radius;
void main() {
    pos_ = pos;
    gl_Position = vec4(pos_ * radius, 0.0, 1.0);
}
`
        );
        if (!vert) {
            throw new Error("failed to compile vertex shader");
        }

        /*
         * dot(N, L) = 1/power <=> cos angle = 1 / power <=> angle = arccos(1/power)
         *
         */
        const frag = this.compileShader(
            gl.FRAGMENT_SHADER,
            `
precision mediump float;

#define PI 3.141592
#define TAU 6.283184

varying vec2 pos_;

uniform sampler2D map;
uniform float xRot;
uniform float yRot;
uniform vec3 sunDir;
uniform float ambientFactor;

#define SUN_POWER 5.0

void main() {
    float x2y2 = dot(pos_, pos_);
    if (x2y2 <= 1.0) {
        vec3 pos = vec3(pos_.x, pos_.y, sqrt(1.0 - x2y2));
        pos.yz = vec2(
            pos.y * cos(xRot) - pos.z * sin(xRot),
            pos.y * sin(xRot) + pos.z * cos(xRot)
        );
        pos.xz = vec2(
            pos.x * cos(yRot) - pos.z * sin(yRot),
            pos.x * sin(yRot) + pos.z * cos(yRot)
        );
        float theta = acos(pos.y);
        float phi = mod(atan(pos.x, pos.z), TAU);
        vec3 mapColor = texture2D(map, vec2(phi / TAU, theta / PI)).rgb;
        float lighting = clamp((dot(pos, -sunDir) * SUN_POWER + 1.0) / 2.0, 0.0, 1.0)
            * (1.0 - ambientFactor)
            + ambientFactor;
        gl_FragColor = vec4(mapColor * lighting, 1.0);
    } else {
        gl_FragColor = vec4(0.0);
    }
}
`
        );
        if (!frag) {
            throw new Error("failed to compile fragment shader");
        }

        const program = this.linkProgram(vert, frag);
        if (!program) {
            throw new Error("failed to compile shader program");
        }
        gl.useProgram(program);

        const posBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, posBuffer);
        gl.bufferData(
            gl.ARRAY_BUFFER,
            new Float32Array([-1.0, -1.0, 1.0, -1.0, -1.0, 1.0, 1.0, 1.0]),
            gl.STATIC_DRAW
        );
        const posAttrib = gl.getAttribLocation(program, "pos");
        gl.enableVertexAttribArray(posAttrib);
        gl.vertexAttribPointer(posAttrib, 2, gl.FLOAT, true, 0, 0);

        /** @type number */
        this.scale_ = 1;
        /** @type {[number, number]} */
        this.radius_ = [1, 1];
        /** @type WebGLUniformLocation */
        this.radiusUniform = gl.getUniformLocation(program, "radius");
        /** @type number */
        this.scale = this.scale_;
        /** @type {[number, number]} */
        this.radius = this.radius_;

        /** @type number */
        this.xRot_ = 0;
        /** @type WebGLUniformLocation */
        this.xRotUniform = gl.getUniformLocation(program, "xRot");
        /** @type number */
        this.xRot = this.xRot_;

        /** @type number */
        this.yRot_ = 0;
        /** @type WebGLUniformLocation */
        this.yRotUniform = gl.getUniformLocation(program, "yRot");
        /** @type number */
        this.yRot = this.yRot_;

        /** @type {[number, number, number]} */
        this.sunDir_ = [0, 0, -1.0];
        /** @type WebGLUniformLocation */
        this.sunDirUniform = gl.getUniformLocation(program, "sunDir");
        /** @type number */
        this.sunDir = this.sunDir_;

        /** @type number */
        this.ambientFactor_ = 1.0;
        /** @type WebGLUniformLocation */
        this.ambientFactorUniform = gl.getUniformLocation(
            program,
            "ambientFactor"
        );
        /** @type number */
        this.ambientFactor = this.ambientFactor_;

        /** @type WebGLUniformLocation */
        this.mapUniform = gl.getUniformLocation(program, "map");
        /** @type WebGLTexture */
        this.mapTexture = gl.createTexture();
        const mapTextureUnit = 0;
        gl.activeTexture(gl.TEXTURE0 + mapTextureUnit);
        gl.bindTexture(gl.TEXTURE_2D, this.mapTexture);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
        gl.uniform1i(this.mapUniform, mapTextureUnit);

        /** @type boolean */
        this.needsRedraw = false;

        /** @type number | null */
        this.clientWidth = null;
        /** @type number | null */
        this.clientHeight = null;
        this.resize();
    }

    updateRadius() {
        const gl = this.gl;
        gl.uniform2f(
            this.radiusUniform,
            this.radius_[0] * this.scale_,
            this.radius_[1] * this.scale_
        );
        this.needsRedraw = true;
    }

    get scale() {
        return this.scale_;
    }

    set scale(value) {
        this.scale_ = value;
        this.updateRadius();
    }

    get radius() {
        return this.radius_;
    }

    set radius(value) {
        this.radius_ = value;
        this.updateRadius();
    }

    get xRot() {
        return this.xRot_;
    }

    set xRot(value) {
        const gl = this.gl;
        this.xRot_ = value;
        gl.uniform1f(this.xRotUniform, value);
        this.needsRedraw = true;
    }

    get yRot() {
        return this.yRot_;
    }

    set yRot(value) {
        const gl = this.gl;
        this.yRot_ = value;
        gl.uniform1f(this.yRotUniform, value);
        this.needsRedraw = true;
    }

    get sunDir() {
        return this.sunDir_;
    }

    set sunDir(value) {
        const gl = this.gl;
        this.sunDir_ = value;
        gl.uniform3f(this.sunDirUniform, value[0], value[1], value[2]);
        this.needsRedraw = true;
    }

    get ambientFactor() {
        return this.ambientFactor_;
    }

    set ambientFactor(value) {
        const gl = this.gl;
        this.ambientFactor_ = value;
        gl.uniform1f(this.ambientFactorUniform, value);
        this.needsRedraw = true;
    }

    setMap(
        /** @type HTMLImageElement */
        mapImage
    ) {
        const gl = this.gl;
        gl.bindTexture(gl.TEXTURE_2D, this.mapTexture);
        gl.texImage2D(
            gl.TEXTURE_2D,
            0,
            gl.RGBA,
            gl.RGBA,
            gl.UNSIGNED_BYTE,
            mapImage
        );
        this.needsRedraw = true;
    }

    redraw() {
        if (!this.needsRedraw) return;
        const gl = this.gl;
        this.needsRedraw = false;
        gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
    }

    normalizeClientCoords(
        /** @type {[number, number]} */
        [clientX, clientY]
    ) {
        const rect = this.canvasElement.getBoundingClientRect();
        return [
            (((clientX - rect.left) / rect.width) * 2 - 1) /
                this.radius[0] /
                this.scale,
            (1 - ((clientY - rect.top) / rect.height) * 2) /
                this.radius[1] /
                this.scale,
        ];
    }

    z(
        /** @type {[number, number]} */
        [x, y]
    ) {
        const x2y2 = x * x + y * y;
        if (x2y2 > 1) return null;
        return Math.sqrt(1 - x2y2);
    }

    anglesToUV(
        /** @type {[number, number]} */
        [theta, phi]
    ) {
        return [phi / (2 * Math.PI), theta / Math.PI];
    }

    anglesToUnrotatedXYZ(
        /** @type {[number, number]} */
        [theta, phi]
    ) {
        const sinTheta = Math.sin(theta);
        return [
            sinTheta * Math.sin(phi),
            Math.cos(theta),
            sinTheta * Math.cos(phi),
        ];
    }

    projectToAnglesWithRots(
        /** @type {[number, number]} */
        clientCoords,
        /** @type {[number, number]} */
        [xRot, yRot]
    ) {
        const [x, y] = this.normalizeClientCoords(clientCoords);
        const z = this.z([x, y]);
        if (z === null) return null;
        const pos = [x, y, z];
        [pos[1], pos[2]] = [
            pos[1] * Math.cos(xRot) - pos[2] * Math.sin(xRot),
            pos[1] * Math.sin(xRot) + pos[2] * Math.cos(xRot),
        ];
        [pos[0], pos[2]] = [
            pos[0] * Math.cos(yRot) - pos[2] * Math.sin(yRot),
            pos[0] * Math.sin(yRot) + pos[2] * Math.cos(yRot),
        ];
        const theta = Math.acos(pos[1]);
        const phi = Math.atan2(pos[0], pos[2]) % (2 * Math.PI);
        return [theta, phi];
    }

    projectToAngles(
        /** @type {[number, number]} */
        clientCoords
    ) {
        return this.projectToAnglesWithRots(clientCoords, [
            this.xRot,
            this.yRot,
        ]);
    }

    rotationBetween(
        /** @type {[number, number, number]} */
        [oldX, oldY, oldZ],
        /** @type {[number, number]} */
        clientCoords
    ) {
        const [x, y] = this.normalizeClientCoords(clientCoords);
        const z = this.z([x, y]);
        if (z === null) return null;
        /*
         *  ox * cos(yRot + dy) + oz * sin(yRot + dy) = x
         * -ox * sin(yRot + dy) + oz * cos(yRot + dy) = oz'
         *
         *  oy * cos(xRot + dx) + oz' * sin(xRot + dx) = y
         * -oy * sin(xRot + dx) + oz' * cos(xRot + dx) = z
         *
         *  sin(yRot + dy + atan2(ox, oz)) = x/sqrt(ox^2 + oz^2)
         *  So:
         *  yRot + dy + atan2(ox, oz) = asin(x/sqrt(ox^2 + oz^2))
         *
         *  sin(xRot + dx + atan2(oy, oz')) = y/sqrt(oy^2 + oz'^2)
         *  cos(xRot + dx + atan2(oy, oz')) = z/sqrt(oy^2 + oz'^2)
         *  So:
         *  xRot + dx + atan2(oy, oz') = atan2(y, z)
         */
        const sqrtox2oz2 = Math.sqrt(oldX * oldX + oldZ * oldZ);
        const newYRot =
            Math.asin(clamp(x / sqrtox2oz2, -1, 1)) - Math.atan2(oldX, oldZ);
        const oldZ_ = -oldX * Math.sin(newYRot) + oldZ * Math.cos(newYRot);
        const newXRot = clamp(
            Math.atan2(y, z) - Math.atan2(oldY, oldZ_),
            -Math.PI / 2,
            Math.PI / 2
        );
        return [newXRot, newYRot];
    }

    resize() {
        const gl = this.gl;
        const clientWidth = this.canvasElement.clientWidth;
        const clientHeight = this.canvasElement.clientHeight;
        if (
            clientWidth === this.clientWidth &&
            clientHeight === this.clientHeight
        ) {
            return;
        }
        this.clientWidth = clientWidth;
        this.clientHeight = clientHeight;
        const dpr = window.devicePixelRatio;
        const fbWidth = clientWidth * dpr;
        const fbHeight = clientHeight * dpr;
        const minSize = Math.min(clientWidth, clientHeight);
        this.radius = [minSize / clientWidth, minSize / clientHeight];
        this.canvasElement.width = fbWidth;
        this.canvasElement.height = fbHeight;
        gl.viewport(0, 0, fbWidth, fbHeight);
    }
}

class Globe {
    constructor(
        /** @type {{
            rotationSpeed: number;
            zoomSpeed: number;
            ambientFactor: number;
            ambientLightTransitionZoom: number;
            ambientLightTransitionZoomMargin: number;
        }} */
        settings,
        /** @type HTMLCanvasElement */
        canvasElement,
        /** @type string */
        mapImageURL,
        /** @type string */
        regionMapImageURL
    ) {
        /** @type {{
            rotationSpeed: number;
            zoomSpeed: number;
            ambientFactor: number;
            ambientLightTransitionZoom: number;
            ambientLightTransitionZoomMargin: number;
        }} */
        this.settings = settings;

        /** @type HTMLCanvasElement */
        this.canvasElement = canvasElement;

        /** @type GlobeCanvas */
        this.canvas = new GlobeCanvas(this.canvasElement);

        /** @type {ImageData | null} */
        this.regionMapImageData = null;
        const regionMapImage = new Image();
        regionMapImage.src = regionMapImageURL;
        regionMapImage.addEventListener("load", () => {
            const canvas = document.createElement("canvas");
            const ctx = canvas.getContext("2d");
            canvas.width = regionMapImage.width;
            canvas.height = regionMapImage.height;
            ctx.drawImage(regionMapImage, 0, 0);
            this.regionMapImageData = ctx.getImageData(
                0,
                0,
                regionMapImage.width,
                regionMapImage.height
            );
        });

        /** @type HTMLImageElement */
        this.mapImage = new Image();
        this.mapImage.src = mapImageURL;
        this.mapImage.addEventListener("load", () => {
            this.canvas.setMap(this.mapImage);
        });

        this.mount();

        /** @type number */
        this.time_ = null;
        this.time = 0;

        this.scale = 1;

        requestAnimationFrame(this.drawFrame);
    }

    /** @type {[number, number] | null} */
    dragStartClientCoords = null;
    /** @type {[number, number, number] | null} */
    dragStartXYZ = null;
    /** @type boolean */
    isDragging = false;

    click = (
        /** @type MouseEvent */
        e
    ) => {
        if (!this.regionMapImageData || this.isDragging) return;
        const angles = this.canvas.projectToAngles([e.clientX, e.clientY]);
        if (!angles) return;
        const [u, v] = this.canvas.anglesToUV(angles);
        const [x, y] = [
            Math.floor(u * this.regionMapImageData.width),
            Math.floor(v * this.regionMapImageData.height),
        ];
        const startIndex = 4 * (y * this.regionMapImageData.width + x);
        const [r, g, b, a] = this.regionMapImageData.data.slice(
            startIndex,
            startIndex + 4
        );
        console.log("Clicked", r, g, b, a);
    };

    mouseDown = (
        /** @type MouseEvent */
        e
    ) => {
        const dragStartAngles = this.canvas.projectToAngles([
            e.clientX,
            e.clientY,
        ]);
        if (!dragStartAngles) return;
        this.dragStartClientCoords = [e.clientX, e.clientY];
        this.dragStartXYZ = this.canvas.anglesToUnrotatedXYZ(dragStartAngles);
        this.isDragging = false;
    };

    mouseUp = () => {
        this.dragStartClientCoords = null;
        this.dragStartXYZ = null;
    };

    static DRAG_THRESHOLD = 4;

    mouseMove = (
        /** @type MouseEvent */
        e
    ) => {
        if (!this.dragStartXYZ) return;
        const dx = e.clientX - this.dragStartClientCoords[0];
        const dy = e.clientY - this.dragStartClientCoords[1];
        if (dx * dx + dy * dy >= Globe.DRAG_THRESHOLD) {
            this.isDragging = true;
        }
        const newRots = this.canvas.rotationBetween(this.dragStartXYZ, [
            e.clientX,
            e.clientY,
        ]);
        if (!newRots) return;
        this.canvas.xRot = newRots[0];
        this.canvas.yRot = newRots[1];
    };

    wheel = (
        /** @type WheelEvent */
        e
    ) => {
        e.preventDefault();
        const startAngles = this.canvas.projectToAngles([e.clientX, e.clientY]);
        this.scale = Math.max(
            this.scale * (1 - e.deltaY * this.settings.zoomSpeed),
            0.1
        );
        if (!startAngles) return;
        const startXYZ = this.canvas.anglesToUnrotatedXYZ(startAngles);
        const newRots = this.canvas.rotationBetween(startXYZ, [
            e.clientX,
            e.clientY,
        ]);
        if (!newRots) return;
        this.canvas.xRot = newRots[0];
        this.canvas.yRot = newRots[1];
    };

    mount() {
        this.canvasElement.addEventListener("click", this.click);
        this.canvasElement.addEventListener("mousedown", this.mouseDown);
        window.addEventListener("mouseup", this.mouseUp);
        this.canvasElement.addEventListener("mousemove", this.mouseMove);
        this.canvasElement.addEventListener("wheel", this.wheel);
    }

    unmount() {
        this.canvasElement.removeEventListener("click", this.click);
        this.canvasElement.removeEventListener("mousedown", this.mouseDown);
        window.removeEventListener("mouseup", this.mouseUp);
        this.canvasElement.removeEventListener("mousemove", this.mouseMove);
        this.canvasElement.removeEventListener("wheel", this.wheel);
    }

    animateRotation(
        /** @type number */
        elapsedSecs
    ) {
        if (this.settings.rotationSpeed && !this.dragStartXYZ) {
            this.canvas.yRot += elapsedSecs * this.settings.rotationSpeed;
        }
    }

    get time() {
        return this.time_;
    }

    set time(value) {
        if (value === this.time_) return;
        this.time_ = value;
        const angle = value * 2 * Math.PI;
        this.canvas.sunDir = [Math.cos(angle), 0, Math.sin(angle)];
    }

    get scale() {
        return this.canvas.scale;
    }

    set scale(value) {
        this.canvas.scale = value;
        this.updateAmbientFactor();
    }

    updateAmbientFactor() {
        const transitionFactor = Math.sin(
            clamp(
                (this.canvas.scale - this.settings.ambientLightTransitionZoom) /
                    this.settings.ambientLightTransitionZoomMargin,
                -1,
                1
            ) *
                (Math.PI / 2)
        );
        this.canvas.ambientFactor =
            ((transitionFactor + 1) / 2) * (1 - this.settings.ambientFactor) +
            this.settings.ambientFactor;
    }

    /** @type {number | null} */
    prevFrameTime = null;

    startFrame() {
        const frameTime = performance.now();
        const elapsedSecs =
            this.prevFrameTime === null
                ? 0
                : (frameTime - this.prevFrameTime) * 0.001;
        this.prevFrameTime = frameTime;
        return elapsedSecs;
    }

    drawFrame = () => {
        const elapsedSecs = this.startFrame();
        this.animateRotation(elapsedSecs);
        this.canvas.resize();
        this.canvas.redraw();
        requestAnimationFrame(this.drawFrame);
    };
}

const globe = new Globe(
    {
        rotationSpeed: 0,
        zoomSpeed: 1 / 1500,
        ambientFactor: 0.5,
        ambientLightTransitionZoom: 2,
        ambientLightTransitionZoomMargin: 1,
    },
    document.getElementById("canvas"),
    "/img/map.png",
    "/img/map.png"
);

const updateTime = () => {
    const date = new Date();
    const earthTime =
        ((date.getUTCSeconds() / 60 + date.getUTCMinutes()) / 60 +
            date.getUTCHours()) /
        24;
    globe.time = earthTime;
};
updateTime();
setInterval(updateTime, 1000);

function clamp(value, min, max) {
    return value < min ? min : value > max ? max : value;
}

class MapChunker {
    constructor(
        /** @type WebGLRenderingContext */
        gl
    ) {
        const maxTextureSize = gl.getParameter(gl.MAX_TEXTURE_SIZE);

        /** @type HTMLCanvasElement */
        this.canvas = document.createElement("canvas");
        this.canvas.width = maxTextureSize;
        this.canvas.height = maxTextureSize;
        /** @type CanvasRenderingContext2D */
        this.ctx = this.canvas.getContext("2d");
        /** @type WebGLRenderingContext */
        this.gl = gl;

        /** @type number */
        this.maxTextureSize = maxTextureSize;

        /** @type {HTMLImageElement | null} */
        this.map_ = null;
        /** @type {[number, number] | null} */
        this.maxDivs = null;
        /** @type {[number, number] | null} */
        this.chunkLevel_ = null;
        /** @type {[number, number] | null} */
        this.chunkSrcSize = null;
        /** @type {[number, number] | null} */
        this.chunkDstSize = null;
        /** @type {[[WebGLTexture | null]] | null} */
        this.chunks = null;
    }

    get map() {
        return this.map_;
    }

    set map(value) {
        if (value === this.map_) return;
        this.map_ = value;
        this.maxDivs = this.map_
            ? [
                  Math.max(Math.ceil(this.map_.width / this.maxTextureSize), 2),
                  Math.max(
                      Math.ceil(this.map_.height / this.maxTextureSize),
                      2
                  ),
              ]
            : null;
        while (this.map_.width % this.maxDivs[0] !== 0) {
            this.maxDivs[0]++;
        }
        while (this.map_.height % this.maxDivs[1] !== 0) {
            this.maxDivs[1]++;
        }
        this.chunkLevel_ = null;
        this.resetChunks();
    }

    get chunkLevel() {
        return this.chunkLevel_;
    }

    set chunkLevel(value) {
        if (
            value === this.chunkLevel_ ||
            (value &&
                this.chunkLevel_ &&
                value[0] === this.chunkLevel_[0] &&
                value[1] === this.chunkLevel_[1])
        )
            return;
        this.chunkLevel_ = value;
        this.resetChunks();
    }

    chunkLevelForView(
        /** @type {[number, number]} */
        [viewWidth, viewHeight]
    ) {
        let xDivs = Math.min(Math.floor(1 / viewWidth), this.maxDivs[0]);
        let yDivs = Math.min(Math.floor(1 / viewHeight), this.maxDivs[1]);
        while (this.map_.width % xDivs !== 0) {
            xDivs--;
        }
        while (this.map_.height % yDivs !== 0) {
            yDivs--;
        }
        return [xDivs, yDivs];
    }

    chunkAtUV(
        /** @type {[number, number]} */
        [x, y]
    ) {
        return [
            Math.floor(x * this.chunkLevel_[0]),
            Math.floor(y * this.chunkLevel_[1]),
        ];
    }

    chunkUV(
        /** @type {[number, number]} */
        [xI, yI]
    ) {
        return [xI / this.chunkLevel_[0], yI / this.chunkLevel_[1]];
    }

    chunkCenterUV(
        /** @type {[number, number]} */
        [xI, yI]
    ) {
        return [
            (xI + 0.5) / this.chunkLevel_[0],
            (yI + 0.5) / this.chunkLevel_[1],
        ];
    }

    resetChunks() {
        if (this.chunks) {
            for (const row of this.chunks) {
                for (const chunk of row) {
                    if (!chunk) continue;
                    this.gl.deleteTexture(chunk);
                }
            }
        }
        this.chunks = null;
        if (this.map_ && this.chunkLevel_) {
            this.chunkSrcSize = [
                this.map_.width / this.chunkLevel_[0],
                this.map_.height / this.chunkLevel_[1],
            ];
            this.chunkDstSize = [
                Math.min(this.chunkSrcSize[0], this.maxTextureSize),
                Math.min(this.chunkSrcSize[1], this.maxTextureSize),
            ];
        } else {
            this.chunkSrcSize = null;
            this.chunkDstSize = null;
        }
    }

    bindChunk(
        /** @type {[number, number]} */
        [xI, yI]
    ) {
        const gl = this.gl;

        if (!this.chunks) {
            this.chunks = [];
            for (let y = 0; y < this.chunkLevel_[1]; y++) {
                this.chunks.push(new Array(this.chunkLevel_[0]).fill(null));
            }
        }

        /** @type WebGLTexture */
        let texture;
        if ((texture = this.chunks[yI][xI])) {
            gl.bindTexture(gl.TEXTURE_2D, texture);
            return;
        }

        const chunkX = (xI * this.map_.width) / this.chunkLevel_[0];
        const chunkY = (yI * this.map_.height) / this.chunkLevel_[1];
        this.ctx.drawImage(
            this.map_,
            chunkX,
            chunkY,
            this.chunkSrcSize[0],
            this.chunkSrcSize[1],
            0,
            0,
            this.chunkDstSize[0],
            this.chunkDstSize[1]
        );
        const data = this.ctx.getImageData(
            0,
            0,
            this.chunkDstSize[0],
            this.chunkDstSize[1]
        );

        texture = gl.createTexture();
        gl.bindTexture(gl.TEXTURE_2D, texture);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
        gl.texImage2D(
            gl.TEXTURE_2D,
            0,
            gl.RGBA,
            gl.RGBA,
            gl.UNSIGNED_BYTE,
            data
        );

        this.chunks[yI][xI] = texture;
    }
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

uniform vec2 scaledRadius;

void main() {
    pos_ = pos;
    gl_Position = vec4(pos_ * scaledRadius, 0.0, 1.0);
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

uniform sampler2D mapTL;
uniform sampler2D mapTR;
uniform sampler2D mapBL;
uniform sampler2D mapBR;
uniform vec2 mapChunksStart;
uniform vec2 mapChunksLen;

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
        
        vec2 chunkOffset = fract(vec2(phi / TAU, theta / PI) - mapChunksStart) * mapChunksLen;
        float chunkYOffset = chunkOffset.x >= 2.0 ? 1.0 - chunkOffset.y : chunkOffset.y;
        vec3 mapColor;
        if (chunkYOffset < 1.0) {
            if (chunkOffset.x < 1.0) {
                mapColor = texture2D(mapTL, fract(chunkOffset)).rgb;
            } else {
                mapColor = texture2D(mapTR, fract(chunkOffset)).rgb;
            }
        } else if (chunkOffset.x < 1.0) {
            mapColor = texture2D(mapBL, fract(chunkOffset)).rgb;
        } else {
            mapColor = texture2D(mapBR, fract(chunkOffset)).rgb;
        }
        
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

        /** @type {MapChunker} */
        this.mapChunker = new MapChunker(gl);
        /** @type {[number, number] | null} */
        this.mapChunksStart = null;
        /** @type WebGLUniformLocation */
        this.mapChunksStartUniform = gl.getUniformLocation(
            program,
            "mapChunksStart"
        );
        /** @type WebGLUniformLocation */
        this.mapChunksLenUniform = gl.getUniformLocation(
            program,
            "mapChunksLen"
        );

        /** @type number */
        this.scale_ = 1;
        /** @type {[number, number]} */
        this.radius_ = [1, 1];
        /** @type {[number, number]} */
        this.scaledRadius_ = [1, 1];
        /** @type WebGLUniformLocation */
        this.scaledRadiusUniform = gl.getUniformLocation(
            program,
            "scaledRadius"
        );
        this.updateScaledRadius();

        /** @type number */
        this.xRot_ = 0;
        /** @type WebGLUniformLocation */
        this.xRotUniform = gl.getUniformLocation(program, "xRot");
        this.updateXRot();

        /** @type number */
        this.yRot_ = 0;
        /** @type WebGLUniformLocation */
        this.yRotUniform = gl.getUniformLocation(program, "yRot");
        this.updateYRot();

        /** @type {[number, number, number]} */
        this.sunDir_ = [0, 0, -1.0];
        /** @type WebGLUniformLocation */
        this.sunDirUniform = gl.getUniformLocation(program, "sunDir");
        this.updateSunDir();

        /** @type number */
        this.ambientFactor_ = 1.0;
        /** @type WebGLUniformLocation */
        this.ambientFactorUniform = gl.getUniformLocation(
            program,
            "ambientFactor"
        );
        this.updateAmbientFactor();

        /** @type {[WebGLUniformLocation, WebGLUniformLocation, WebGLUniformLocation, WebGLUniformLocation]} */
        this.mapUniforms = [
            gl.getUniformLocation(program, "mapTL"),
            gl.getUniformLocation(program, "mapTR"),
            gl.getUniformLocation(program, "mapBL"),
            gl.getUniformLocation(program, "mapBR"),
        ];
        for (let i = 0; i < 4; i++) {
            gl.uniform1i(this.mapUniforms[i], i);
        }

        /** @type boolean */
        this.needsRedraw = false;

        /** @type number | null */
        this.clientWidth = null;
        /** @type number | null */
        this.clientHeight = null;
        this.resize();
    }

    updateScaledRadius() {
        const gl = this.gl;
        gl.uniform2f(
            this.scaledRadiusUniform,
            this.scaledRadius_[0],
            this.scaledRadius_[1]
        );
        this.needsRedraw = true;
        this.updateChunks();
    }

    get scaledRadius() {
        return this.scaledRadius_;
    }

    set scaledRadius(value) {
        if (
            value[0] === this.scaledRadius_[0] &&
            value[1] === this.scaledRadius_[1]
        ) {
            return;
        }
        this.scaledRadius_ = value;
        this.updateScaledRadius();
    }

    get scale() {
        return this.scale_;
    }

    set scale(value) {
        this.scale_ = value;
        this.scaledRadius = [
            this.radius_[0] * this.scale_,
            this.radius_[1] * this.scale_,
        ];
    }

    get radius() {
        return this.radius_;
    }

    set radius(value) {
        this.radius_ = value;
        this.scaledRadius = [
            this.radius_[0] * this.scale_,
            this.radius_[1] * this.scale_,
        ];
    }

    updateXRot() {
        const gl = this.gl;
        gl.uniform1f(this.xRotUniform, this.xRot_);
        this.needsRedraw = true;
        this.updateChunks();
    }

    get xRot() {
        return this.xRot_;
    }

    set xRot(value) {
        if (value === this.xRot_) return;
        this.xRot_ = value;
        this.updateXRot();
    }

    updateYRot() {
        const gl = this.gl;
        gl.uniform1f(this.yRotUniform, this.yRot_);
        this.needsRedraw = true;
        this.updateChunks();
    }

    get yRot() {
        return this.yRot_;
    }

    set yRot(value) {
        if (value === this.yRot_) return;
        this.yRot_ = value;
        this.updateYRot();
    }

    updateSunDir() {
        const gl = this.gl;
        gl.uniform3f(
            this.sunDirUniform,
            this.sunDir_[0],
            this.sunDir_[1],
            this.sunDir_[2]
        );
        this.needsRedraw = true;
    }

    get sunDir() {
        return this.sunDir_;
    }

    set sunDir(value) {
        if (
            value[0] === this.sunDir_[0] &&
            value[1] === this.sunDir_[1] &&
            value[2] === this.sunDir_[2]
        ) {
            return;
        }
        this.sunDir_ = value;
        this.updateSunDir();
    }

    updateAmbientFactor() {
        const gl = this.gl;
        gl.uniform1f(this.ambientFactorUniform, this.ambientFactor_);
        this.needsRedraw = true;
    }

    get ambientFactor() {
        return this.ambientFactor_;
    }

    set ambientFactor(value) {
        if (value === this.ambientFactor_) return;
        this.ambientFactor_ = value;
        this.updateAmbientFactor();
    }

    updateMap() {
        this.needsRedraw = true;
        this.updateChunks();
    }

    get map() {
        return this.mapChunker.map;
    }

    set map(value) {
        if (value === this.mapChunker.map) return;
        this.mapChunker.map = value;
        this.mapChunksStart = null;
        this.updateMap();
    }

    updateChunks() {
        if (!this.mapChunker.map) {
            this.mapChunksStart = null;
            return;
        }

        const gl = this.gl;

        const [cornerX, cornerY] = [
            1 / this.scaledRadius_[0],
            1 / this.scaledRadius_[1],
        ];
        const cornerZ = this.z([cornerX, cornerY]);
        const viewXAngle = cornerZ
            ? Math.atan(cornerX / cornerZ)
            : Math.PI * 0.5;
        const viewYAngle = cornerZ
            ? Math.atan(cornerY / cornerZ)
            : Math.PI * 0.5;
        const viewWidth = viewXAngle / Math.PI;
        const viewHeight = Math.min((2 * viewYAngle) / Math.PI, 0.5);

        const chunkLevel = this.mapChunker.chunkLevelForView([
            viewWidth,
            viewHeight,
        ]);
        this.mapChunker.chunkLevel = chunkLevel;

        if (!this.mapChunker.chunks) {
            gl.uniform2f(
                this.mapChunksLenUniform,
                chunkLevel[0],
                chunkLevel[1]
            );
        }

        const center = this.anglesToUV(
            this.projectNormalizedToAnglesWithRots(
                [0, 0],
                [this.xRot, this.yRot]
            )
        );
        const centerChunk = this.mapChunker.chunkAtUV(center, chunkLevel);
        const centerChunkCenter = this.mapChunker.chunkCenterUV(
            centerChunk,
            chunkLevel
        );
        let chunkIndices;
        if (centerChunkCenter[1] < center[1]) {
            if (centerChunkCenter[0] < center[0]) {
                // Center chunk on the top left
                chunkIndices = [
                    centerChunk,
                    [centerChunk[0] + 1, centerChunk[1]],
                    [centerChunk[0], centerChunk[1] + 1],
                    [centerChunk[0] + 1, centerChunk[1] + 1],
                ];
            } else {
                // Center chunk on the top right
                chunkIndices = [
                    [centerChunk[0] - 1, centerChunk[1]],
                    centerChunk,
                    [centerChunk[0] - 1, centerChunk[1] + 1],
                    [centerChunk[0], centerChunk[1] + 1],
                ];
            }
        } else if (centerChunkCenter[0] < center[0]) {
            // Center chunk on the bottom left
            chunkIndices = [
                [centerChunk[0], centerChunk[1] - 1],
                [centerChunk[0] + 1, centerChunk[1] - 1],
                centerChunk,
                [centerChunk[0] + 1, centerChunk[1]],
            ];
        } else {
            // Center chunk on the bottom right
            chunkIndices = [
                [centerChunk[0] - 1, centerChunk[1] - 1],
                [centerChunk[0], centerChunk[1] - 1],
                [centerChunk[0] - 1, centerChunk[1]],
                centerChunk,
            ];
        }

        const chunksStart = this.mapChunker.chunkUV(
            chunkIndices[0],
            chunkLevel
        );
        const chunksStartChanged =
            !this.mapChunksStart ||
            chunksStart[0] !== this.mapChunksStart[0] ||
            chunksStart[1] !== this.mapChunksStart[1];
        if (chunksStartChanged) {
            this.mapChunksStart = chunksStart;
            gl.uniform2f(
                this.mapChunksStartUniform,
                chunksStart[0],
                chunksStart[1]
            );
        }

        if (this.mapChunker.chunks && !chunksStartChanged) return;

        for (let i = 0; i < 4; i++) {
            gl.activeTexture(gl.TEXTURE0 + i);
            const xI = (chunkIndices[i][0] + chunkLevel[0]) % chunkLevel[0];
            const yI =
                chunkLevel[1] -
                1 -
                Math.abs(chunkLevel[1] - 1 - Math.abs(chunkIndices[i][1]));
            this.mapChunker.bindChunk([xI, yI]);
        }
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

    projectNormalizedToAnglesWithRots(
        /** @type {[number, number]} */
        [x, y],
        /** @type {[number, number]} */
        [xRot, yRot]
    ) {
        const z = this.z([x, y]);
        if (z === null) return null;
        const xRotSin = Math.sin(xRot);
        const xRotCos = Math.cos(xRot);
        const yRotSin = Math.sin(yRot);
        const yRotCos = Math.cos(yRot);
        const pos = [x, y, z];
        [pos[1], pos[2]] = [
            pos[1] * xRotCos - pos[2] * xRotSin,
            pos[1] * xRotSin + pos[2] * xRotCos,
        ];
        [pos[0], pos[2]] = [
            pos[0] * yRotCos - pos[2] * yRotSin,
            pos[0] * yRotSin + pos[2] * yRotCos,
        ];
        const theta = Math.acos(pos[1]);
        const phi = Math.atan2(pos[0], pos[2]);
        return [
            (theta + Math.PI) % Math.PI,
            (phi + 2 * Math.PI) % (2 * Math.PI),
        ];
    }

    projectToAnglesWithRots(
        /** @type {[number, number]} */
        clientCoords,
        /** @type {[number, number]} */
        rots
    ) {
        return this.projectNormalizedToAnglesWithRots(
            this.normalizeClientCoords(clientCoords),
            rots
        );
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
            ambientZoom: number;
            ambientZoomMargin: number;
            axialTilt: number;
            sunBaseAngle: number;
        }} */
        settings,
        /** @type HTMLCanvasElement */
        canvasElement,
        /** @type string */
        mapImageURL,
        /** @type string */
        regionMapImageURL,
        /** @type {(r: number, g: number, b: number, a: number) => void} */
        clickHandler,
        /** @type {[number, number]} */
        [startXRot, startYRot] = [0, 0]
    ) {
        /** @type {{
            rotationSpeed: number;
            zoomSpeed: number;
            ambientFactor: number;
            ambientZoom: number;
            ambientZoomMargin: number;
            axialTilt: number;
            sunBaseAngle: number;
        }} */
        this.settings = settings;

        /** @type HTMLCanvasElement */
        this.canvasElement = canvasElement;

        /** @type GlobeCanvas */
        this.canvas = new GlobeCanvas(this.canvasElement);
        this.canvas.xRot = startXRot;
        this.canvas.yRot = startYRot;

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
            this.canvas.map = this.mapImage;
        });

        this.mount();

        /** @type number */
        this.axialTiltSin = Math.sin(this.settings.axialTilt);
        /** @type number */
        this.axialTiltCos = Math.cos(this.settings.axialTilt);
        /** @type number */
        this.time_ = null;
        this.time = 0;

        this.scale = 1;

        /** @type {(r: number, g: number, b: number, a: number) => void} */
        this.clickHandler = clickHandler;
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
        this.clickHandler(r, g, b, a);
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
        e.preventDefault();
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

    dragStart = (
        /** @type MouseEvent */
        e
    ) => {
        e.preventDefault();
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
        this.canvasElement.addEventListener("dragstart", this.dragStart);
        this.canvasElement.addEventListener("wheel", this.wheel);
    }

    unmount() {
        this.canvasElement.removeEventListener("click", this.click);
        this.canvasElement.removeEventListener("mousedown", this.mouseDown);
        window.removeEventListener("mouseup", this.mouseUp);
        this.canvasElement.removeEventListener("mousemove", this.mouseMove);
        this.canvasElement.removeEventListener("dragstart", this.dragStart);
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
        const sunAngle = value * 2 * Math.PI + this.settings.sunBaseAngle;
        const mSinSunAngle = -Math.sin(sunAngle);
        this.canvas.sunDir = [
            this.axialTiltCos * mSinSunAngle,
            this.axialTiltSin * mSinSunAngle,
            -Math.cos(sunAngle),
        ];
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
                (this.canvas.scale - this.settings.ambientZoom) /
                    this.settings.ambientZoomMargin,
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

    startRendering() {
        requestAnimationFrame(this.drawFrame);
    }
}

const globe = new Globe(
    {
        rotationSpeed: 0,
        zoomSpeed: 1 / 1500,
        ambientFactor: 0.5,
        ambientZoom: 2,
        ambientZoomMargin: 1,
        axialTilt: (23.5 * Math.PI) / 180,
        sunBaseAngle: -0.196 * 2 * Math.PI,
    },
    document.getElementById("canvas"),
    "img/map.png",
    "img/color.png",
    clickRedirect,
    [-0.2, 3.35]
);

// Click redirection

const regionColors = {
    nusea: [218, 92, 98],
    acrion: [203, 166, 247],
    kestun: [180, 190, 254],
    litea: [245, 224, 220],
    hestria: [15, 101, 74],
    tinar: [140, 232, 109],
};

function clickRedirect(r, g, b, a) {
    if (a > 100) {
        for (const regionName in regionColors) {
            const [rr, rg, rb] = regionColors[regionName];
            if (r === rr && g === rg && b === rb) {
                window.location.href = `https://${regionName}.she-a.eu`;
                break;
            }
        }
    }
}

// Time handling

onWASMLoad(
    CyraTime,
    (CyraTime) => {
        if (CyraTime) {
            const curtime = CyraTime.cwrap("curtime", null, ["number"]);
            const updateTime = () => {
                const outputBytes = CyraTime._malloc(50);
                curtime(outputBytes);
                /** @type string */
                const output = CyraTime.UTF8ToString(outputBytes).slice(0, -1);
                CyraTime._free(outputBytes);

                const [, timeStr] = output.split(" ");
                const [hourStr, halfHourStr, [minuteStr, secondStr]] = [
                    timeStr[0],
                    timeStr[1],
                    timeStr.substring(2).split(":"),
                ];
                const hour = parseInt(hourStr, 16);
                const minute =
                    parseInt(minuteStr, 8) + 60 * +(halfHourStr === "x");
                const second = parseInt(secondStr, 10);
                globe.time = ((second / 46 + minute) / 120 + hour) / 16;
            };
            updateTime();
            setInterval(updateTime, 1000);
        }
        globe.startRendering();
    },
    {
        arguments: ["e"],
    }
);

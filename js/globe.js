function clamp(value, min, max) {
	return value < min ? min : value > max ? max : value;
}

class Canvas {
	compileShader(kind, source) {
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

	linkProgram(vert, frag) {
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
		gl_FragColor = texture2D(map, vec2(phi / TAU, theta / PI));
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
			new Float32Array([
				-1.0, -1.0, 1.0, -1.0, -1.0, 1.0, 1.0, 1.0,
			]),
			gl.STATIC_DRAW
		);
		const posAttrib = gl.getAttribLocation(program, "pos");
		gl.enableVertexAttribArray(posAttrib);
		gl.vertexAttribPointer(posAttrib, 2, gl.FLOAT, true, 0, 0);

		/** @type number */
		this.scale_ = 0.9;
		/** @type [number, number] */
		this.radius_ = [1, 1];
		/** @type WebGLUniformLocation */
		this.radiusUniform = gl.getUniformLocation(
			program,
			"radius"
		);
		/** @type number */
		this.scale = this.scale_;
		/** @type [number, number] */
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

		/** @type WebGLUniformLocation */
		this.mapUniform = gl.getUniformLocation(program, "map");
		/** @type WebGLTexture */
		this.mapTexture = gl.createTexture();
		const mapTextureUnit = 0;
		gl.activeTexture(gl.TEXTURE0 + mapTextureUnit);
		gl.bindTexture(gl.TEXTURE_2D, this.mapTexture);
		gl.texParameteri(
			gl.TEXTURE_2D,
			gl.TEXTURE_WRAP_S,
			gl.CLAMP_TO_EDGE
		);
		gl.texParameteri(
			gl.TEXTURE_2D,
			gl.TEXTURE_WRAP_T,
			gl.CLAMP_TO_EDGE
		);
		gl.texParameteri(
			gl.TEXTURE_2D,
			gl.TEXTURE_MIN_FILTER,
			gl.LINEAR
		);
		gl.uniform1i(this.mapUniform, mapTextureUnit);

		/** @type boolean */
		this.needsRedraw = false;
	}

	updateRadius() {
		const gl = this.gl;
		gl.uniform2f(
			this.radiusUniform,
			this.radius_[0] * this.scale_,
			this.radius_[1] * this.scale_,
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

	normalizeClientCoords(clientX, clientY) {
		const rect = this.canvasElement.getBoundingClientRect();
		return [
			(((clientX - rect.left) / rect.width) * 2 - 1) /
				this.radius[0] / this.scale,
			(1 - ((clientY - rect.top) / rect.height) * 2) /
				this.radius[1] / this.scale,
		];
	}

	projectWithRots(clientX, clientY, xRot, yRot) {
		const [x, y] = this.normalizeClientCoords(clientX, clientY);
		const x2y2 = x * x + y * y;
		if (x2y2 > 1) return null;
		const z = Math.sqrt(1 - x2y2);
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

	project(clientX, clientY) {
		return this.projectWithRots(
			clientX,
			clientY,
			this.xRot,
			this.yRot
		);
	}

	rotationBetween(
		oldTheta,
		oldPhi,
		clientX,
		clientY,
		xRot,
		yRot
	) {
		const [x, y] = this.normalizeClientCoords(clientX, clientY);
		const x2y2 = x * x + y * y;
		if (x2y2 > 1) return null;
		const z = Math.sqrt(1 - x2y2);
		const sinOldTheta = Math.sin(oldTheta);
		const [oldX, oldY, oldZ] = [
			sinOldTheta * Math.sin(oldPhi),
			Math.cos(oldTheta),
			sinOldTheta * Math.cos(oldPhi),
		];
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
		const ox2oz2 = oldX * oldX + oldZ * oldZ;
		const newYRot =
			Math.asin(clamp(x / Math.sqrt(ox2oz2), -1, 1)) -
			Math.atan2(oldX, oldZ);
		const oldZ_ =
			-oldX * Math.sin(newYRot) + oldZ * Math.cos(newYRot);
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
		const dpr = window.devicePixelRatio;
		const fbWidth = clientWidth * dpr;
		const fbHeight = clientHeight * dpr;
		const minSize = Math.min(clientWidth, clientHeight);
		this.radius = [
			minSize / clientWidth,
			minSize / clientHeight,
		];
		this.canvasElement.width = fbWidth;
		this.canvasElement.height = fbHeight;
		gl.viewport(0, 0, fbWidth, fbHeight);
	}
}

/** @type HTMLCanvasElement */
const canvasElement = document.getElementById("canvas");

/** @type Canvas */
const canvas = new Canvas(canvasElement);
canvas.resize();

const mapImage = new Image();
mapImage.src = "./img/map.png";
mapImage.onload = () => {
	canvas.setMap(mapImage);
};

//const rotationSpeed = Math.PI / 12;
const rotationSpeed = 0;
let prevFrameTime = performance.now();
const drawFrame = () => {
	const frameTime = performance.now();
	const elapsedSecs = (frameTime - prevFrameTime) * 0.001;
	prevFrameTime = frameTime;
	if (rotationSpeed !== null && !dragStartPos) {
		canvas.yRot += elapsedSecs * rotationSpeed;
	}

	canvas.redraw();
	requestAnimationFrame(drawFrame);
};
requestAnimationFrame(drawFrame);

let dragStartPos = null;
let dragStartRots = null;
canvasElement.addEventListener("mousedown", (e) => {
	dragStartPos = canvas.project(e.clientX, e.clientY);
	if (!dragStartPos) return;
	dragStartRots = [canvas.xRot, canvas.yRot];
});
window.addEventListener("mouseup", () => {
	dragStartPos = null;
	dragStartRots = null;
});
canvasElement.addEventListener("mousemove", (e) => {
	if (!dragStartPos) return;
	pos = [];
	const newRots = canvas.rotationBetween(
		dragStartPos[0],
		dragStartPos[1],
		e.clientX,
		e.clientY,
		dragStartRots[0],
		dragStartRots[1]
	);
	if (!newRots) return;
	canvas.xRot = newRots[0];
	canvas.yRot = newRots[1];
});
canvasElement.addEventListener("wheel", (e) => {
	canvas.scale = Math.max(canvas.scale * (1 - e.deltaY / 1500), 0.1);
	e.preventDefault();
});
window.addEventListener("resize", (e) => {
	canvas.resize();
});


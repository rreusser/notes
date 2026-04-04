

// TypeScript declarations for @stdlib/lapack/base/dgbsvx

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a real system A * X = B where A is general band, with equilibration, condition estimation, and error bounds.
	*/
	(
		fact: string,
		trans: string,
		N: number,
		kl: number,
		ku: number,
		nrhs: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		AFB: Float64Array,
		strideAFB1: number,
		strideAFB2: number,
		offsetAFB: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		equed: string,
		r: Float64Array,
		strideR: number,
		offsetR: number,
		c: Float64Array,
		strideC: number,
		offsetC: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		X: Float64Array,
		strideX1: number,
		strideX2: number,
		offsetX: number,
		rcond: number,
		FERR: Float64Array,
		strideFERR: number,
		offsetFERR: number,
		BERR: Float64Array,
		strideBERR: number,
		offsetBERR: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		IWORK: Int32Array,
		strideIWORK: number,
		offsetIWORK: number
	): Float64Array;
}

/**
* Solves a real system A * X = B where A is general band, with equilibration, condition estimation, and error bounds.
*/
declare var dgbsvx: Routine;

export = dgbsvx;

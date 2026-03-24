

// TypeScript declarations for @stdlib/lapack/base/zgesvx

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Expert driver for solving complex general linear systems
	*/
	(
		fact: string,
		trans: string,
		N: number,
		nrhs: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		AF: Float64Array,
		strideAF1: number,
		strideAF2: number,
		offsetAF: number,
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
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number
	): Float64Array;
}

/**
* Expert driver for solving complex general linear systems
*/
declare var zgesvx: Routine;

export = zgesvx;

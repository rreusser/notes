

// TypeScript declarations for @stdlib/lapack/base/dgtsvx

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Expert driver for solving a general tridiagonal system
	*/
	(
		fact: string,
		trans: string,
		N: number,
		nrhs: number,
		DL: Float64Array,
		strideDL: number,
		offsetDL: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		DU: Float64Array,
		strideDU: number,
		offsetDU: number,
		DLF: Float64Array,
		strideDLF: number,
		offsetDLF: number,
		DF: Float64Array,
		strideDF: number,
		offsetDF: number,
		DUF: Float64Array,
		strideDUF: number,
		offsetDUF: number,
		DU2: Float64Array,
		strideDU2: number,
		offsetDU2: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
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
* Expert driver for solving a general tridiagonal system
*/
declare var dgtsvx: Routine;

export = dgtsvx;

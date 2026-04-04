

// TypeScript declarations for @stdlib/lapack/base/ztrrfs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Provides error bounds for the solution to a system with a complex triangular matrix.
	*/
	(
		uplo: string,
		trans: string,
		diag: string,
		N: number,
		nrhs: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		X: Float64Array,
		strideX1: number,
		strideX2: number,
		offsetX: number,
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
* Provides error bounds for the solution to a system with a complex triangular matrix.
*/
declare var ztrrfs: Routine;

export = ztrrfs;

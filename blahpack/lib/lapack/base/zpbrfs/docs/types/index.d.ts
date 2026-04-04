

// TypeScript declarations for @stdlib/lapack/base/zpbrfs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Improves the computed solution to a complex system A * X = B where A is Hermitian positive definite band and provides error bounds.
	*/
	(
		uplo: string,
		N: number,
		kd: number,
		nrhs: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		AFB: Float64Array,
		strideAFB1: number,
		strideAFB2: number,
		offsetAFB: number,
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
* Improves the computed solution to a complex system A * X = B where A is Hermitian positive definite band and provides error bounds.
*/
declare var zpbrfs: Routine;

export = zpbrfs;

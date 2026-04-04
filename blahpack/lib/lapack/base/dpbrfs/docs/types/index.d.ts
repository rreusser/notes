

// TypeScript declarations for @stdlib/lapack/base/dpbrfs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Improves the computed solution to a real system A * X = B where A is symmetric positive definite band and provides error bounds.
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
		IWORK: Int32Array,
		strideIWORK: number,
		offsetIWORK: number
	): Float64Array;
}

/**
* Improves the computed solution to a real system A * X = B where A is symmetric positive definite band and provides error bounds.
*/
declare var dpbrfs: Routine;

export = dpbrfs;

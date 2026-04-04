

// TypeScript declarations for @stdlib/lapack/base/zhbgst

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reduces a complex Hermitian-definite banded generalized eigenproblem to standard form.
	*/
	(
		vect: string,
		uplo: string,
		N: number,
		ka: number,
		kb: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		BB: Float64Array,
		strideBB1: number,
		strideBB2: number,
		offsetBB: number,
		X: Float64Array,
		strideX1: number,
		strideX2: number,
		offsetX: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number
	): Float64Array;
}

/**
* Reduces a complex Hermitian-definite banded generalized eigenproblem to standard form.
*/
declare var zhbgst: Routine;

export = zhbgst;

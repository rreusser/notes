

// TypeScript declarations for @stdlib/lapack/base/zgeqp3

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* QR factorization with column pivoting (driver)
	*/
	(
		M: number,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		JPVT: Int32Array,
		strideJPVT: number,
		offsetJPVT: number,
		TAU: Float64Array,
		strideTAU: number,
		offsetTAU: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number,
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number
	): Float64Array;
}

/**
* QR factorization with column pivoting (driver)
*/
declare var zgeqp3: Routine;

export = zgeqp3;

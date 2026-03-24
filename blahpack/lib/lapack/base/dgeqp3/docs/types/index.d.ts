

// TypeScript declarations for @stdlib/lapack/base/dgeqp3

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes a QR factorization with column pivoting of a real matrix
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
		lwork: number
	): Float64Array;
}

/**
* Computes a QR factorization with column pivoting of a real matrix
*/
declare var dgeqp3: Routine;

export = dgeqp3;

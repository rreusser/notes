

// TypeScript declarations for @stdlib/lapack/base/dgeqr2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute QR factorization of a real matrix (unblocked).
	*/
	(
		M: number,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		TAU: Float64Array,
		strideTAU: number,
		offsetTAU: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Compute QR factorization of a real matrix (unblocked).
*/
declare var dgeqr2: Routine;

export = dgeqr2;



// TypeScript declarations for @stdlib/lapack/base/zung2r

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generate unitary matrix Q from QR factorization (unblocked)
	*/
	(
		M: number,
		N: number,
		K: number,
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
* Generate unitary matrix Q from QR factorization (unblocked)
*/
declare var zung2r: Routine;

export = zung2r;

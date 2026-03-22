

// TypeScript declarations for @stdlib/lapack/base/zungl2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generate unitary matrix Q from LQ factorization (unblocked)
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
* Generate unitary matrix Q from LQ factorization (unblocked)
*/
declare var zungl2: Routine;

export = zungl2;

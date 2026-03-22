

// TypeScript declarations for @stdlib/lapack/base/zunglq

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generate unitary matrix Q from LQ factorization (blocked)
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
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Generate unitary matrix Q from LQ factorization (blocked)
*/
declare var zunglq: Routine;

export = zunglq;

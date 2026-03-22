

// TypeScript declarations for @stdlib/lapack/base/zunml2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Apply unitary matrix from LQ factorization (unblocked)
	*/
	(
		side: string,
		trans: string,
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
		C: Float64Array,
		strideC1: number,
		strideC2: number,
		offsetC: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Apply unitary matrix from LQ factorization (unblocked)
*/
declare var zunml2: Routine;

export = zunml2;

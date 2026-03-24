

// TypeScript declarations for @stdlib/lapack/base/zunmr2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Multiplies a general matrix by the unitary matrix Q from an RQ factorization (unblocked)
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
* Multiplies a general matrix by the unitary matrix Q from an RQ factorization (unblocked)
*/
declare var zunmr2: Routine;

export = zunmr2;

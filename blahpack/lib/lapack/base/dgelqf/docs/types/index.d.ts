

// TypeScript declarations for @stdlib/lapack/base/dgelqf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the LQ factorization of a real matrix (blocked)
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
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Compute the LQ factorization of a real matrix (blocked)
*/
declare var dgelqf: Routine;

export = dgelqf;

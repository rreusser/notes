

// TypeScript declarations for @stdlib/lapack/base/zgerqf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Complex blocked RQ factorization
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
* Complex blocked RQ factorization
*/
declare var zgerqf: Routine;

export = zgerqf;

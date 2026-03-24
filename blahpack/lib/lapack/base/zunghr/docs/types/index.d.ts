

// TypeScript declarations for @stdlib/lapack/base/zunghr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generates the unitary matrix Q from Hessenberg reduction
	*/
	(
		N: number,
		ilo: number,
		ihi: number,
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
* Generates the unitary matrix Q from Hessenberg reduction
*/
declare var zunghr: Routine;

export = zunghr;



// TypeScript declarations for @stdlib/lapack/base/zunmhr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Multiplies a matrix by the unitary matrix Q from Hessenberg reduction
	*/
	(
		side: string,
		trans: string,
		M: number,
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
		C: Float64Array,
		strideC1: number,
		strideC2: number,
		offsetC: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Multiplies a matrix by the unitary matrix Q from Hessenberg reduction
*/
declare var zunmhr: Routine;

export = zunmhr;

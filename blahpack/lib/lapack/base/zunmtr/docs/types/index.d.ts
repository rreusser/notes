

// TypeScript declarations for @stdlib/lapack/base/zunmtr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Applies a complex unitary matrix from zhetrd to a matrix
	*/
	(
		side: string,
		uplo: string,
		trans: string,
		M: number,
		N: number,
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
* Applies a complex unitary matrix from zhetrd to a matrix
*/
declare var zunmtr: Routine;

export = zunmtr;

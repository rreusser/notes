

// TypeScript declarations for @stdlib/lapack/base/zunmbr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Apply unitary matrices from bidiagonal reduction
	*/
	(
		vect: string,
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
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Apply unitary matrices from bidiagonal reduction
*/
declare var zunmbr: Routine;

export = zunmbr;

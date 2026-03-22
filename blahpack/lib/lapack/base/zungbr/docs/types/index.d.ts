

// TypeScript declarations for @stdlib/lapack/base/zungbr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generate unitary matrices Q and P^H from bidiagonal reduction
	*/
	(
		vect: string,
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
* Generate unitary matrices Q and P^H from bidiagonal reduction
*/
declare var zungbr: Routine;

export = zungbr;

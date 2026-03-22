

// TypeScript declarations for @stdlib/lapack/base/zlatrd

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reduce NB rows and columns of a Hermitian matrix to tridiagonal form
	*/
	(
		uplo: string,
		N: number,
		nb: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		TAU: Float64Array,
		strideTAU: number,
		offsetTAU: number,
		W: Float64Array,
		strideW1: number,
		strideW2: number,
		offsetW: number
	): Float64Array;
}

/**
* Reduce NB rows and columns of a Hermitian matrix to tridiagonal form
*/
declare var zlatrd: Routine;

export = zlatrd;

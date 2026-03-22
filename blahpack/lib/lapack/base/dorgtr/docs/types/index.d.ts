

// TypeScript declarations for @stdlib/lapack/base/dorgtr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generate the orthogonal matrix from a tridiagonal reduction
	*/
	(
		uplo: string,
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
* Generate the orthogonal matrix from a tridiagonal reduction
*/
declare var dorgtr: Routine;

export = dorgtr;

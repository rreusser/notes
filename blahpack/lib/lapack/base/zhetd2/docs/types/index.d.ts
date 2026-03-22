

// TypeScript declarations for @stdlib/lapack/base/zhetd2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reduce a Hermitian matrix to tridiagonal form (unblocked)
	*/
	(
		uplo: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		TAU: Float64Array,
		strideTAU: number,
		offsetTAU: number
	): Float64Array;
}

/**
* Reduce a Hermitian matrix to tridiagonal form (unblocked)
*/
declare var zhetd2: Routine;

export = zhetd2;

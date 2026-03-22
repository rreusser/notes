

// TypeScript declarations for @stdlib/lapack/base/dlansy

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the norm of a real symmetric matrix
	*/
	(
		norm: string,
		uplo: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Compute the norm of a real symmetric matrix
*/
declare var dlansy: Routine;

export = dlansy;

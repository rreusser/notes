

// TypeScript declarations for @stdlib/lapack/base/zlanhe

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the norm of a Hermitian matrix
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
* Compute the norm of a Hermitian matrix
*/
declare var zlanhe: Routine;

export = zlanhe;

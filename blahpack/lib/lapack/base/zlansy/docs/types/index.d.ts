

// TypeScript declarations for @stdlib/lapack/base/zlansy

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Complex symmetric matrix norm
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
* Complex symmetric matrix norm
*/
declare var zlansy: Routine;

export = zlansy;



// TypeScript declarations for @stdlib/lapack/base/zsyr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform complex symmetric rank-1 update
	*/
	(
		uplo: string,
		N: number,
		alpha: any,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number
	): Float64Array;
}

/**
* Perform complex symmetric rank-1 update
*/
declare var zsyr: Routine;

export = zsyr;

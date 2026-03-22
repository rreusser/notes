

// TypeScript declarations for @stdlib/blas/base/dsyr2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform symmetric rank-2 update
	*/
	(
		uplo: string,
		N: number,
		alpha: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		y: Float64Array,
		strideY: number,
		offsetY: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number
	): Float64Array;
}

/**
* Perform symmetric rank-2 update
*/
declare var dsyr2: Routine;

export = dsyr2;

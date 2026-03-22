

// TypeScript declarations for @stdlib/blas/base/dsyr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform the symmetric rank 1 operation A := alpha*x*x**T + A
	*/
	(
		uplo: string,
		N: number,
		alpha: number,
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
* Perform the symmetric rank 1 operation A := alpha*x*x**T + A
*/
declare var dsyr: Routine;

export = dsyr;

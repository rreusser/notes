

// TypeScript declarations for @stdlib/blas/base/dtrsm

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Double-precision real triangular solve with multiple right-hand sides.
	*/
	(
		side: string,
		uplo: string,
		transa: string,
		diag: string,
		M: number,
		N: number,
		alpha: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Double-precision real triangular solve with multiple right-hand sides.
*/
declare var dtrsm: Routine;

export = dtrsm;

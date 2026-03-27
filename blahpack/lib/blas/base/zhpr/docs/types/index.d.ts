

// TypeScript declarations for @stdlib/blas/base/zhpr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform the Hermitian packed rank-1 update A := alpha*x*x**H + A.
	*/
	(
		uplo: string,
		N: number,
		alpha: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number
	): Float64Array;
}

/**
* Perform the Hermitian packed rank-1 update A := alpha*x*x**H + A.
*/
declare var zhpr: Routine;

export = zhpr;

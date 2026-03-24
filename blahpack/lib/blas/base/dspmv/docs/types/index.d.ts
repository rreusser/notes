

// TypeScript declarations for @stdlib/blas/base/dspmv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform matrix-vector operation with a symmetric packed matrix
	*/
	(
		uplo: string,
		N: number,
		alpha: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		beta: number,
		y: Float64Array,
		strideY: number,
		offsetY: number
	): Float64Array;
}

/**
* Perform matrix-vector operation with a symmetric packed matrix
*/
declare var dspmv: Routine;

export = dspmv;



// TypeScript declarations for @stdlib/blas/base/zhpmv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform the Hermitian packed matrix-vector operation y := alpha*A*x + beta*y.
	*/
	(
		uplo: string,
		N: number,
		alpha: any,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		beta: any,
		y: Float64Array,
		strideY: number,
		offsetY: number
	): Float64Array;
}

/**
* Perform the Hermitian packed matrix-vector operation y := alpha*A*x + beta*y.
*/
declare var zhpmv: Routine;

export = zhpmv;

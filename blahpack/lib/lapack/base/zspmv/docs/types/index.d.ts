

// TypeScript declarations for @stdlib/lapack/base/zspmv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform the symmetric packed matrix-vector operation y := alpha*A*x + beta*y where A is a complex symmetric matrix stored in packed format
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
* Perform the symmetric packed matrix-vector operation y := alpha*A*x + beta*y where A is a complex symmetric matrix stored in packed format
*/
declare var zspmv: Routine;

export = zspmv;

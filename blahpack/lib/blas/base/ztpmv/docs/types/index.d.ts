

// TypeScript declarations for @stdlib/blas/base/ztpmv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform one of the triangular packed matrix-vector operations x := A*x or x := A**T*x or x := A**H*x.
	*/
	(
		uplo: string,
		trans: string,
		diag: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		x: Float64Array,
		strideX: number,
		offsetX: number
	): Float64Array;
}

/**
* Perform one of the triangular packed matrix-vector operations x := A*x or x := A**T*x or x := A**H*x.
*/
declare var ztpmv: Routine;

export = ztpmv;

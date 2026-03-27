

// TypeScript declarations for @stdlib/blas/base/ztpsv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve one of the triangular packed systems A*x = b or A**T*x = b or A**H*x = b.
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
* Solve one of the triangular packed systems A*x = b or A**T*x = b or A**H*x = b.
*/
declare var ztpsv: Routine;

export = ztpsv;

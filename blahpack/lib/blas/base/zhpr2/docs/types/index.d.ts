

// TypeScript declarations for @stdlib/blas/base/zhpr2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform the Hermitian packed rank-2 update A := alpha*x*y**H + conj(alpha)*y*x**H + A.
	*/
	(
		uplo: string,
		N: number,
		alpha: any,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		y: Float64Array,
		strideY: number,
		offsetY: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number
	): Float64Array;
}

/**
* Perform the Hermitian packed rank-2 update A := alpha*x*y**H + conj(alpha)*y*x**H + A.
*/
declare var zhpr2: Routine;

export = zhpr2;

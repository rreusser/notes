

// TypeScript declarations for @stdlib/blas/base/dtpsv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a triangular packed system of equations
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
* Solve a triangular packed system of equations
*/
declare var dtpsv: Routine;

export = dtpsv;

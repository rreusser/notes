

// TypeScript declarations for @stdlib/blas/base/dnrm2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the Euclidean norm of a real vector.
	*/
	(
		N: number,
		x: Float64Array,
		stride: number,
		offset: number
	): Float64Array;
}

/**
* Compute the Euclidean norm of a real vector.
*/
declare var dnrm2: Routine;

export = dnrm2;

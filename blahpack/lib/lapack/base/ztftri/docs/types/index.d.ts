

// TypeScript declarations for @stdlib/lapack/base/ztftri

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the inverse of a complex triangular matrix in Rectangular Full Packed format.
	*/
	(
		transr: string,
		uplo: string,
		diag: string,
		N: number,
		a: Float64Array,
		stride: number,
		offset: number
	): Float64Array;
}

/**
* Computes the inverse of a complex triangular matrix in Rectangular Full Packed format.
*/
declare var ztftri: Routine;

export = ztftri;

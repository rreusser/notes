

// TypeScript declarations for @stdlib/lapack/base/dtftri

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the inverse of a real triangular matrix in Rectangular Full Packed format.
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
* Computes the inverse of a real triangular matrix in Rectangular Full Packed format.
*/
declare var dtftri: Routine;

export = dtftri;

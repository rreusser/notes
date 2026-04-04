

// TypeScript declarations for @stdlib/lapack/base/zlanhf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Returns the norm of a complex Hermitian matrix in Rectangular Full Packed format.
	*/
	(
		norm: string,
		transr: string,
		uplo: string,
		N: number,
		a: Float64Array,
		strideA: number,
		offsetA: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Returns the norm of a complex Hermitian matrix in Rectangular Full Packed format.
*/
declare var zlanhf: Routine;

export = zlanhf;

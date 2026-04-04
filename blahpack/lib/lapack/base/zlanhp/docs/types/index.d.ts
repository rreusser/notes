

// TypeScript declarations for @stdlib/lapack/base/zlanhp

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Returns the norm of a complex Hermitian matrix in packed storage.
	*/
	(
		norm: string,
		uplo: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Returns the norm of a complex Hermitian matrix in packed storage.
*/
declare var zlanhp: Routine;

export = zlanhp;

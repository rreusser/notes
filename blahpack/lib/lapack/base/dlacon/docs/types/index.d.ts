

// TypeScript declarations for @stdlib/lapack/base/dlacon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimates the 1-norm of a square real matrix using reverse communication.
	*/
	(
		N: number,
		v: Float64Array,
		strideV: number,
		offsetV: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		ISGN: Int32Array,
		strideISGN: number,
		offsetISGN: number,
		EST: Float64Array,
		KASE: Int32Array
	): void;
}

/**
* Estimates the 1-norm of a square real matrix using reverse communication.
*/
declare var dlacon: Routine;

export = dlacon;

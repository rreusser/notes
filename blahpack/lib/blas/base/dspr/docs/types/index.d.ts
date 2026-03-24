

// TypeScript declarations for @stdlib/blas/base/dspr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform symmetric rank-1 update of a packed matrix
	*/
	(
		uplo: string,
		N: number,
		alpha: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number
	): Float64Array;
}

/**
* Perform symmetric rank-1 update of a packed matrix
*/
declare var dspr: Routine;

export = dspr;

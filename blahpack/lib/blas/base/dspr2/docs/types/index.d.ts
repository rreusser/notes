

// TypeScript declarations for @stdlib/blas/base/dspr2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform symmetric rank-2 update of a packed matrix
	*/
	(
		uplo: string,
		N: number,
		alpha: number,
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
* Perform symmetric rank-2 update of a packed matrix
*/
declare var dspr2: Routine;

export = dspr2;

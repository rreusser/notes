

// TypeScript declarations for @stdlib/lapack/base/zspr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform the symmetric rank-1 update of a complex symmetric packed matrix
	*/
	(
		uplo: string,
		N: number,
		alpha: any,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number
	): Float64Array;
}

/**
* Perform the symmetric rank-1 update of a complex symmetric packed matrix
*/
declare var zspr: Routine;

export = zspr;

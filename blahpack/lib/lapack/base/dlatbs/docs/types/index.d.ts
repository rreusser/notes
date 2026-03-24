

// TypeScript declarations for @stdlib/lapack/base/dlatbs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a triangular banded system with scaling for overflow
	*/
	(
		uplo: string,
		trans: string,
		diag: string,
		normin: string,
		N: number,
		kd: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		scale: number,
		CNORM: Float64Array,
		strideCNORM: number,
		offsetCNORM: number
	): Float64Array;
}

/**
* Solves a triangular banded system with scaling for overflow
*/
declare var dlatbs: Routine;

export = dlatbs;

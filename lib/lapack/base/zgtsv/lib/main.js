'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgtsv = require( './zgtsv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgtsv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgtsv;

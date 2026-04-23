'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgbsv = require( './zgbsv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgbsv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgbsv;

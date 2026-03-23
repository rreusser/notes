

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgbsv = require( './dgbsv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgbsv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgbsv;


'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dorbdb4 = require( './dorbdb4.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorbdb4, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorbdb4;

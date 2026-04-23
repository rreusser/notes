
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsptrs = require( './zsptrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsptrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsptrs;

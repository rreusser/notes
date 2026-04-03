
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztbtrs = require( './ztbtrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztbtrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztbtrs;


'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztftri = require( './ztftri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztftri, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztftri;



'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var ztrtrs = require( './ztrtrs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztrtrs, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztrtrs;

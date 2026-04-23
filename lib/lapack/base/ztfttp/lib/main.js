'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztfttp = require( './ztfttp.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztfttp, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztfttp;


'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztrevc = require( './ztrevc.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztrevc, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztrevc;

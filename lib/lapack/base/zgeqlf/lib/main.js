
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgeqlf = require( './zgeqlf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgeqlf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgeqlf;

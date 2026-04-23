
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztgsja = require( './ztgsja.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztgsja, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztgsja;



'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var ztgevc = require( './ztgevc.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztgevc, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztgevc;

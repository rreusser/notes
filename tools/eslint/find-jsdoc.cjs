'use strict';

// Copied from @stdlib/_tools/eslint/utils/find-jsdoc, with @stdlib deps replaced.

// VARIABLES //

var RE_FUNCTION = /Function/;


// FUNCTIONS //

function isCommentToken( token ) {
	return (
		token.type === 'Line' ||
		token.type === 'Block' ||
		token.type === 'Shebang'
	);
}


// MAIN //

/**
* Retrieves the JSDoc comment associated with a given AST node.
*
* @param {Source} source - source code
* @param {ASTNode} node - AST node
* @returns {(Token|null)} block comment token or null
*/
function jsdoc( source, node ) {
	var comment;
	var parent;
	var type;

	parent = node.parent;
	type = node.type;

	if ( type === 'ClassDeclaration' || type === 'FunctionDeclaration' ) {
		return findToken( node );
	}
	if ( type === 'VariableDeclaration' || type === 'ExpressionStatement' ) {
		comment = findToken( node );
		if ( comment && comment.value.charAt( 0 ) === '*' ) {
			return comment;
		}
		return null;
	}
	if ( type === 'ClassExpression' ) {
		return findToken( parent.parent );
	}
	if ( type === 'ArrowFunctionExpression' || type === 'FunctionExpression' ) {
		if ( parent.type !== 'CallExpression' && parent.type !== 'NewExpression' ) {
			while (
				!source.getCommentsBefore( parent ).length &&
				!RE_FUNCTION.test( parent.type ) &&
				parent.type !== 'MethodDefinition' &&
				parent.type !== 'Property'
			) {
				parent = parent.parent;
				if ( !parent ) {
					break;
				}
			}
			if (
				parent &&
				parent.type !== 'FunctionDeclaration' &&
				parent.type !== 'Program'
			) {
				return findToken( parent );
			}
		}
		return findToken( node );
	}
	return null;

	function findToken( node ) {
		var prev;
		var opts;
		var incr;

		opts = {
			'includeComments': true
		};
		prev = source.getTokenBefore( node, opts );
		incr = 1;
		if (
			prev &&
			isCommentToken( prev ) &&
			prev.type === 'Line'
		) {
			opts.skip = 1;
			prev = source.getTokenBefore( node, opts );
			incr += 3;
		}
		if (
			prev &&
			isCommentToken( prev ) &&
			prev.type === 'Block' &&
			prev.value.charAt( 0 ) === '*' &&
			node.loc.start.line-prev.loc.end.line <= incr
		) {
			return prev;
		}
		return null;
	}
}


// EXPORTS //

module.exports = jsdoc;
